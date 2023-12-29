import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IRobotTypeController from '../../controllers/IControllers/IRobotTypeController';

import config from "../../../config";
import middlewares from "../middlewares";
import UserRole from "../../enums/userRole";

const route = Router();

export default (app: Router) => {
  app.use('/robots/types',
    middlewares.authRequest([
      UserRole.ADMINISTRATOR.toString(),
      UserRole.FLEET_MANAGER.toString()
    ]),
    route);

  const ctrl = Container.get(config.controllers.robotType.name) as IRobotTypeController;

  route.post('',
    celebrate({
      body: Joi.object({
        designacao: Joi.string().required(),
        tipoTarefas: Joi.array().items(Joi.string()).required()
      })
    }),
    (req, res, next) => ctrl.createRobotType(req, res, next));

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        designacao: Joi.string().required(),
        tipoTarefas: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updateRobotType(req, res, next));

  route.get('/search',
    (req, res, next) => {
      ctrl.findByDesignationOrTaskType(req, res, next);
  });

  route.get('',
    (req, res, next) => {
      ctrl.getAllRobotTypes(req, res, next);
  });

}
