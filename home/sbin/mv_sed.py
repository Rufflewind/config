import abc
import dataclasses
import datetime
import enum
import itertools
import os
import pathlib
import re
import subprocess
import sys
from typing import (
    Callable,
    Dict,
    Iterable,
    Iterator,
    List,
    Mapping,
    MutableMapping,
    Sequence,
    Set,
    Tuple,
    Type,
    TypeVar,
    Union
)

class Action(enum.Enum):
    NOP = enum.auto()
    RENAME = enum.auto()

class Error(Exception):
    pass

T = TypeVar("T")

def lookup_with_path_compression(
        parents: MutableMapping[T, T],
        key: T,
) -> T:
    ancestors = []
    parent = parents.get(key, key)
    while True:
        grandparent = parents.get(parent, parent)
        if grandparent == parent:
            root = grandparent
            break
        ancestors.append(key)
        key, parent = parent, grandparent
    for ancestor in ancestors:
        parents[ancestor] = root
    return root

def find_conflicting_paths(
        paths: Sequence[pathlib.PurePath],
) -> Mapping[int, int]:
    """
    Precondition: All paths must be resolved (i.e. realpaths) otherwise false
      negatives can occur.

    Returns: [(conflict_index, cause_index)]
    """
    conflict_to_cause: Dict[int, int] = {}
    path_to_index: Dict[pathlib.PurePath, int] = {}
    for i, path in enumerate(paths):
        if path in path_to_index:
            conflict_to_cause[i] = path_to_index[path]
        else:
            path_to_index[path] = i
    for i, path in enumerate(paths):
        for ancestor in path.parents:
            if ancestor in path_to_index:
                conflict_to_cause[i] = path_to_index[ancestor]
                break
    return {
        conflict: lookup_with_path_compression(conflict_to_cause, conflict)
        for conflict in frozenset(itertools.chain(
            conflict_to_cause,
            conflict_to_cause.values(),
        ))
    }

def validate_mass_rename(
        olds: Sequence[pathlib.Path],
        news: Sequence[pathlib.Path],
) -> Sequence[Tuple[Union[Action, str], pathlib.Path, pathlib.Path]]:
    reals = [new.resolve() for new in news]
    conflicts = find_conflicting_paths(reals)
    actions: List[Tuple[
        Union[Action, str],
        pathlib.Path,
        pathlib.Path,
    ]] = []
    for i, (old, new, real) in enumerate(zip(olds, news, reals)):
        real_parent = real.parent
        action: Union[Action, str]
        if i in conflicts:
            action = f"DEST_CONFLICT:{news[conflicts[i]]}"
        elif not os.path.lexists(old):
            action = "SOURCE_NOT_EXIST"
        elif old == new:
            action = Action.NOP
        elif not real_parent.exists():
            action = "DEST_PARENT_NOT_EXIST"
        elif not real_parent.is_dir():
            action = "DEST_PARENT_NOT_DIR"
        elif os.path.lexists(real):
            action = "DEST_EXISTS"
        else:
            action = Action.RENAME
        actions.append((action, old, new))
    for old in olds[len(news):]:
        actions.append(("DEST_NOT_SET", old, old))
    for new in news[len(olds):]:
        actions.append(("SOURCE_NOT_SET", new, new))
    return actions

def mv_transform(
        dry_run: bool,
        transformation: Callable[[Sequence[str]], Sequence[str]],
        files: Sequence[str],
) -> None:
    seen: Set[pathlib.Path] = set()
    unique_files: List[str] = []
    for path in files:
        real = pathlib.Path(path).resolve()
        if real not in seen:
            unique_files.append(path)
            seen.add(real)

    try:
        transformed_files = transformation(unique_files)
    except subprocess.CalledProcessError as e:
        raise Error(e)

    olds = list(map(pathlib.Path, unique_files))
    news = list(map(pathlib.Path, transformed_files))
    actions = validate_mass_rename(olds, news)
    num_errors = sum(not isinstance(action, Action) for action, _, _ in actions)
    if num_errors:
        dry_run = True

    table = []
    for action, old, new in actions:
        if isinstance(action, Action):
            action_str = action.name.lower()
        else:
            action_str = action
        if action == Action.RENAME and dry_run:
            action_str += "_dry_run"
        table.append((action_str, str(old), str(new)))

    msgs = []
    widths = [max(map(len, col)) for col in zip(*table)]
    for row in table:
        line = " ".join(cell.ljust(width) for width, cell in zip(widths, row))
        msgs.append(line.rstrip() + "\n")

    for (action, old, new), msg in zip(actions, msgs):
        sys.stdout.write(msg)
        sys.stdout.flush()
        if action == Action.RENAME and not dry_run:
            if os.path.lexists(new):
                raise Error(f"destination already exists: {new!r}")
            os.rename(old, new)

    if num_errors:
        raise Error(f"aborting due to {num_errors} errors")

DATE_PARTS = {
    "B": ("month", r".+?"),
    "H": ("hour", r"(?a:\d{2})"),
    "M": ("minute", r"(?a:\d{2})"),
    "S": ("second", r"(?a:\d{2})"),
    "Y": ("year", r"(?a:\d{4})"),
    "b": ("month", r".+?"),
    "d": ("day", r"(?a:\d{1,2})"),
    "m": ("month", r"(?a:\d{1,2})"),
    "y": ("year", r"(?a:\d{2})"),
}

@dataclasses.dataclass
class DatePattern:
    date_patterns: "DatePatterns"
    variable: str

    def __format__(self, format_spec: str) -> str:
        match = re.fullmatch(r"%(\w)", format_spec)
        if not match:
            raise ValueError(
                f"format_spec must be of form '%<letter>', not {format_spec!r}")
        [directive] = match.groups()
        part, pattern = DATE_PARTS[directive]
        capture = f"__capture_{part}_{self.variable}"
        if capture in self.date_patterns.captures:
            raise ValueError(
                f"capture for {self.variable}.{part} already exists: "
                f"{{{self.variable}:{format_spec}}}")
        self.date_patterns.captures[capture] = self.variable, directive
        return fr"(?P<{capture}>{pattern})"

@dataclasses.dataclass
class DatePatterns(Mapping[str, DatePattern]):
    captures: Dict[str, Tuple[str, str]]

    def __getitem__(self, key: str) -> DatePattern:
        return DatePattern(self, key)

    def __iter__(self) -> Iterator[str]:
        return iter(())

    def __len__(self) -> int:
        return 0

DEFAULT_DATETIME = datetime.datetime(1970, 1, 1, tzinfo=datetime.timezone.utc)

def date_substituter(pattern: str, replacement: str) -> Callable[[str], str]:
    date_patterns = DatePatterns({})
    pattern = pattern.format_map(date_patterns)
    def replace(match: re.Match) -> str:
        variables = match.groupdict()
        variable_parts = {}
        for capture, (variable, directive) in date_patterns.captures.items():
            parts = variable_parts.setdefault(variable, {})
            parts[directive] = variables[capture]
        for variable, parts in variable_parts.items():
            directives, values = zip(*parts.items())
            fmt = " ".join(map("%{}".format, directives))
            value = " ".join(values)
            variables[variable] = datetime.datetime.strptime(value, fmt)
        return match.expand(replacement.format_map(variables))
    return lambda s: re.sub(pattern, replace, s)

def transform_date(
        pattern_replacements: Iterable[Tuple[str, str]],
        paths: Sequence[str],
) -> Sequence[str]:
    substituters = [date_substituter(pattern, replacement)
                    for pattern, replacement in pattern_replacements]
    new_paths = []
    for path in paths:
        for substituter in substituters:
            path = substituter(path)
        new_paths.append(path)
    return new_paths

def parse_sed_substitutions(script: str) -> Sequence[Tuple[str, str]]:
    substitutions: List[Tuple[str, str]] = []
    while True:
        script = script.lstrip()
        if not script:
            break
        if script[0] != "s":
            raise ValueError("only 's' sed-style command is supported")
        script = script[1:]
        if not script:
            raise ValueError("missing separator after 's'")
        sep = script[0]
        script = script[1:]
        parts = script.split(sep, 2)
        if len(parts) != 3:
            raise ValueError("expected 3 separators after 's'")
        pattern, replacement, script = parts
        match = re.fullmatch(r"(?s)[\s;]*(.*)", script)
        assert match
        [script] = match.groups()
        substitutions.append((pattern, replacement))
    return substitutions

class Transformation(abc.ABC):

    TRANSFORMATIONS: Dict[str, Type["Transformation"]] = {}

    @abc.abstractmethod
    def __call__(self, arg: str, paths: Sequence[str]) -> Sequence[str]:
        pass

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        cls.TRANSFORMATIONS[cls.__name__] = cls

    @classmethod
    def parse(cls, transformation: str) -> "Transformation":
        try:
            transform = eval(transformation, cls.TRANSFORMATIONS)
        except Exception as e:
            raise Error(e)
        if not isinstance(transform, Transformation):
            raise Error("--transform be an instance of Transformation")
        return transform

@dataclasses.dataclass
class Command(Transformation):
    """Transform using a command-line program."""
    command: Sequence[str]

    def __call__(self, arg: str, paths: Sequence[str]) -> Sequence[str]:
        """
        Call self.command + [arg] and feeds each path as a separate line via
        stdin. Transformed paths are read from stdout.
        """
        cmd = [*self.command, arg]
        inp = "".join(path + "\n" for path in paths)
        proc = subprocess.run(
            cmd,
            check=True,
            input=inp,
            stdout=subprocess.PIPE,
            universal_newlines=True,
        )
        return proc.stdout.splitlines()

@dataclasses.dataclass
class Date(Transformation):
    def __call__(self, arg: str, paths: Sequence[str]) -> Sequence[str]:
        return transform_date(parse_sed_substitutions(arg), paths)
