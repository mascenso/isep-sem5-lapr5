import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IRobotController from '../../controllers/IControllers/IRobotController';

import config from "../../../config";
import middlewares from "../middlewares";
import UserRole from "../../enums/userRole";

const route = Router();

export default (app: Router) => {
  app.use('/robots',
    middlewares.authRequest([
      UserRole.ADMINISTRATOR.toString(),
      UserRole.FLEET_MANAGER.toString()
    ]),
    route);

  const ctrl = Container.get(config.controllers.robot.name) as IRobotController;

  route.post('',
    celebrate({
      body: Joi.object({
        nickName: Joi.string().required(),
        robotType: Joi.string().required(),
        serialNumber: Joi.string().required(),
        description: Joi.string().optional(),
        inhibited: Joi.boolean().required()
      })
    }),
    (req, res, next) => ctrl.createRobot(req, res, next));

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        nickName: Joi.string().required(),
        robotType: Joi.string().required(),
        serialNumber: Joi.string().required(),
        description: Joi.string(),
        inhibited: Joi.boolean().required()
      }),
    }),
    (req, res, next) => ctrl.updateRobot(req, res, next));

  route.patch('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        nickName: Joi.string(),
        robotType: Joi.string(),
        serialNumber: Joi.string(),
        description: Joi.string(),
        inhibited: Joi.boolean()
      })
    }),
    (req, res, next) => ctrl.updateRobot(req, res, next) );

  route.get('',
    (req, res, next) => {
      ctrl.getAllRobots(req, res, next);
    });

  route.get('/search',
    (req, res, next) => {
      ctrl.findByDesignationOrTaskType(req, res, next);
    });

}
