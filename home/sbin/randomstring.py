#!/usr/bin/env python
import random, string

def random_str(length=32, chars=string.ascii_letters + string.digits):
    return "".join(random.choice(chars) for _ in range(length))

if __name__ == "__main__":
    help(random_str)
