var map = [
    [/google\.com/, "cs-google"],
    [/mail\.google\.com/, "cs-gmail"],
    [/trello\.com/, "cs-trello"],
    [/tweetdeck\.twitter\.com/, "cs-tweetdeck"]
];

var url = window.location.href;
for (var i = 0; i < map.length; i++) {
    if (map[i][0].test(url)) {
        $("html").addClass(map[i][1]);
    }
}
