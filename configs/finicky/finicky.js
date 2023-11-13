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
        "127.0.0.1",
        "127.0.0.1:*"
      ],
      browser: "/Applications/Arc.app"
    },
    {
      match: [
        "app.slack.com",
        "app.asana.com"
      ],
      browser: "/Applications/Arc.app"
    }
  ]
}
