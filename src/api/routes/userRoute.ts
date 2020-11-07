import { Router, Request, Response } from 'express';
import middlewares from '../middlewares';
var user_controller = require('../../controllers/userController');

const route = Router();

export default (app: Router) => {
  app.use('/users', route);

  route.get('/me', middlewares.isAuth, middlewares.attachCurrentUser, user_controller.getMe);
}
