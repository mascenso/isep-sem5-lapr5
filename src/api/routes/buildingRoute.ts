import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';

import config from "../../../config";
import IBuildingController from "../../controllers/IControllers/IBuildingController";

const route = Router();

export default (app: Router) => {
  app.use('/buildings', route);

  const ctrl = Container.get(config.controllers.building.name) as IBuildingController;

  route.post('',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        dimensions: Joi.string().required(),
        name: Joi.string(),
        description: Joi.string()
      })
    }),
    (req, res, next) => ctrl.createBuilding(req, res, next) );

};
