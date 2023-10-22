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

          maxWidth: Joi.number().required(),
          maxLength: Joi.number().required(),

        name: Joi.string(),
        description: Joi.string()
      })
    }),
    (req, res, next) => ctrl.createBuilding(req, res, next) );

  route.patch('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        code: Joi.string(),
        maxWidth: Joi.number(),
        maxLength: Joi.number(),
        name: Joi.string(),
        description: Joi.string()
      })
    }),
    (req, res, next) => ctrl.updateBuilding(req, res, next) );  

};
