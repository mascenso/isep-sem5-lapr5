import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IRobotController from '../../controllers/IControllers/IRobotController'; 

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/robot', route);

  const ctrl = Container.get(config.controllers.robot.name) as IRobotController;

  route.post('',
    celebrate({
      body: Joi.object({
        designacao: Joi.string().required(),
        tarefas: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createRobot(req, res, next) );

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        designacao: Joi.string().required(),
        tarefas: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updateRobot(req, res, next) );

    route.get('', 
    (req, res, next) => { ctrl.getAllRobots(req, res, next);
  });
};