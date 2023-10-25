import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IBridgeController from '../../controllers/IControllers/IBridgeController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/bridges', route);

  const ctrl = Container.get(config.controllers.bridge.name) as IBridgeController;

  route.post('',
    celebrate({
      body: Joi.object(
        {
        code: Joi.string().required(),
        name: Joi.string().required(),
        floorA: Joi.string().required(),
          floorB: Joi.string().required(),
          //bridgedFloors: Joi.array().items(Joi.string()).required(),
        })
    }),
    (req, res, next) => ctrl.createBridge(req, res, next) );

  route.get('',
    (req, res, next) => { ctrl.getAllBridges(req, res, next);
    });

};
