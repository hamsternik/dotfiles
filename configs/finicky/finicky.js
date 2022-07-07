// Use https://finicky-kickstart.now.sh to generate basic configuration
// Learn more about configuration options: https://github.com/johnste/finicky/wiki/Configuration

module.exports = {
  defaultBrowser: "Safari",
  options: {
    hideIcon: false // Hide the finicky icon from the top bar. Default: false
  },
  handlers: [
    {
      match: [
        "127.0.0.1"
      ],
      browser: "Google Chrome"
    },
    {
      match: [ // Open google.com and *.google.com urls in Google Chrome
        "google.com*", // match google.com urls
        finicky.matchDomains(/.*\.google.com/) // use helper function to match on domain only
      ],
      browser: "Google Chrome"
    },
    {
      match: [
        "www.notion.so/superlayer*" // match only Fluxon::Gambit project in Notion
      ],
      browser: "Google Chrome"
    },
    {
      match: [
        "https://dory.app/c/fluxon.com*"
      ],
      browser: "Google Chrome"
    },
    {
        match: [
            "https://fluxon.latticehq.com/*"
        ],
        browser: "Google Chrome"
    },
  ]
}
