import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import ITaskController from '../../controllers/IControllers/ITaskController';

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/tasks', route);

  const ctrl = Container.get(config.controllers.task.name) as ITaskController;

  route.post('/vigilance',
    celebrate({
      body: Joi.object({
        description: Joi.string().required(),
        buildingId: Joi.string().required(),
        floors: Joi.array().required(),
        startPosition: Joi.array().required(),
        endPosition: Joi.array().required(),
        contactNumber: Joi.number().required(),
        user: Joi.object().required(),
        approved: Joi.boolean().default(false),
        pending: Joi.boolean().default(true),
        planned: Joi.boolean().default(false),
      })
    }),
    (req, res, next) => ctrl.createVigilanceTask(req, res, next));

  route.post('/pickupDelivery',
    celebrate({
      body: Joi.object({
        description: Joi.string().required(),
        pickupLocalization: Joi.object({
          buildingId: Joi.string().required(),
          floor: Joi.object().required(),
          room: Joi.array().required()          //coordinates of room
        }).required(),
        deliveryLocalization: Joi.object({
          buildingId: Joi.string().required(),
          floor: Joi.object().required(),
          room: Joi.array().required()          //coordinates of room
        }).required(),
        contactNumber: Joi.number().required(),
        user: Joi.object().required(),
        deliveryContact: Joi.object({
          name: Joi.string().required(),
          contactNumber: Joi.number().required()
        }).required(),
        pickupContact: Joi.object({
          name: Joi.string().required(),
          contactNumber: Joi.number().required()
        }).required(),
        approved: Joi.boolean().default(false),
        pending: Joi.boolean().default(true),
        planned: Joi.boolean().default(false),
      })
    }),
    (req, res, next) => ctrl.createPickupDeliveryTask(req, res, next));

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        name: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updateTask(req, res, next));

  route.get('',
    (req, res, next) => ctrl.getAllTasks(req, res, next));

  route.get('/pendingVigilance',
    (req, res, next) => ctrl.getAllVigilancePendingTasks(req, res, next));

  route.get('/pendingPickUp',
    (req, res, next) => ctrl.getAllPickupDeliveryPendingTasks(req, res, next));

  route.get('/approvedPickUp',
    (req, res, next) => ctrl.getAllPickupDeliveryApprovedTasks(req, res, next));

  route.get('/approvedVigilance',
    (req, res, next) => ctrl.getAllVigilanceApprovedTasks(req, res, next));


  route.post('/planning/',
    (req, res, next) => {
      console.log('INICIO');
      console.log('Corpo da requisição:', req.body);
      next();
    },
    celebrate({
      body: Joi.object({
        LTasks: Joi.array().items(
          Joi.array().items(
            Joi.string(), // Descrição
            Joi.number(),
            Joi.number(),
            Joi.string(), // Valor intermediário
            Joi.number(),
            Joi.number(),
            Joi.string() // Valor intermediário
          )
        ).required(),
        Ngeracoes: Joi.number().required(),
        dimensaoPop: Joi.number().required(),
        pobCruz: Joi.number().required(),
        pobMut: Joi.number().required(),
        tempoLimite: Joi.number().required(),
        avaliacaoDef: Joi.number().required(),
        nEstabiliz: Joi.number().required(),
      }),
    }),
    (req, res, next) => {
      console.log('FIM ');
      console.log('Corpo da requisição:', req.body);
      ctrl.planearTarefas(req, res, next)
    });


};
