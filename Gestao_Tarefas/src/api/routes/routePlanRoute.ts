import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import ITaskController from '../../controllers/IControllers/ITaskController';
import IRouteController from '../../controllers/IControllers/IRouteController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/route', route);

  const ctrl = Container.get(config.controllers.route.name) as IRouteController;

  route.get('/routePlaning/:piso1/:piso2',
    (req, res, next) => {ctrl.planearRota(req, res, next)});

};
