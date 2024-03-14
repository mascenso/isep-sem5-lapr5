# ISEP-LAPR5-2023/24
# Bulletproof Node.js architecture 🛡️

This repository contains the final "LAPR5" course Project 5 at Instituto Politécnico do Porto - Instituto Superior de Engenharia (ISEP).
Utilizing the Onion architecture, further information can be found in the associated [wiki](https://github.com/mascenso/isep-sem5-lapr5/wiki).

## Development

We use `node` version `18.18.0`

```shell
$ nvm install 18.18.0
```

```shell
$ nvm use 18.18.0
```

The first time, you will need to run

```shell
$ npm install
```

Run the mongodb with the following command to enable visible log output:

```shell
$ docker-compose up
```

or, run the mongodb in detach mode without visible log output:

```shell
$ docker-compose up -d
```

Then just start all the servers with on root

```shell
$ node start.js
```

Or start the server inside folder with

```shell
$ npm run start
```

It uses nodemon for livereloading :peace-fingers:

Afterwards, to stop the mongodb, use the following command:

```shell
$ docker-compose down
```

# API Validation

 By using celebrate the req.body schema becomes clary defined at route level, so even frontend devs can read what an API endpoint expects without need to writting a documentation that can get outdated quickly.

 ```js
 route.post('/signup',
  celebrate({
    body: Joi.object({
      name: Joi.string().required(),
      email: Joi.string().required(),
      password: Joi.string().required(),
    }),
  }),
  controller.signup)
 ```

 **Example error**

 ```json
 {
  "errors": {
    "message": "child \"email\" fails because [\"email\" is required]"
  }
 }
 ```

[Read more about celebrate here](https://github.com/arb/celebrate) and [the Joi validation API](https://github.com/hapijs/joi/blob/v15.0.1/API.md)

# Roadmap
- [x] API Validation layer (Celebrate+Joi)
- [ ] Unit tests examples
- [ ] [Cluster mode](https://softwareontheroad.com/nodejs-scalability-issues?utm_source=github&utm_medium=readme)
- [x] The logging _'layer'_
- [ ] Add ageda dashboard
- [x] Continuous integration with CircleCI 😍
- [ ] Deploys script and docs for AWS Elastic Beanstalk and Heroku
- [ ] Integration test with newman 😉
- [ ] Instructions on typescript debugging with VSCode


# FAQ

 ## Where should I put the FrontEnd code? Is this a good backend for Angular or React or Vue or _whatever_ ?

  [It's not a good idea to have node.js serving static assets a.k.a the frontend](https://softwareontheroad.com/nodejs-scalability-issues?utm_source=github&utm_medium=readme)

  Also, I don't wanna take part in frontend frameworks wars 😅

  Just use the frontend framework you like the most _or hate the less_ it will work 😁

 ## Don't you think you can add X layer to do Y? Why do you still use express if the Serverless Framework is better and it's more reliable?

  I know this is not a perfect architecture but it's the most scalable that I know with less code and headache that I know.

  It's meant for small startups or one-developer army projects.

  I know if you start moving layers into another technology, you will end up with your business/domain logic into npm packages, your routing layer will be pure AWS Lambda functions and your data layer a combination of DynamoDB, Redis, maybe redshift, and Agolia.

  Take a deep breath and go slowly, let the business grow and then scale up your product. You will need a team and talented developers anyway.
