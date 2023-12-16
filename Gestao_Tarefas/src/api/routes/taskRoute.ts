import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import ITaskController from '../../controllers/IControllers/ITaskController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/tasks', route);

  const ctrl = Container.get(config.controllers.task.name) as ITaskController;

  route.post('',
    celebrate({
      body: Joi.object({
        name: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createTask(req, res, next) );

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        name: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updateTask(req, res, next) );

  route.get('',
    (req, res, next) => ctrl.getAllTasks(req, res, next) );

};
