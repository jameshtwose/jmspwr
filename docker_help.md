# Jmspwr deployment help
`jmspwr` uses docker, github actions, and shinyapps.io to deploy. Prior to attempting to push to main it is suggested to try building and running the docker image locally.

### Docker
1. Build the docker image: `docker build -t jmspwr .`
2. Test the image locally: `docker run --publish 5000:5000 jmspwr` here the 5000:5000 is referring the port that the app is served from and to (from:to) - make sure this matches in your `options()` in the `app.R` file 