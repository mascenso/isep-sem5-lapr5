import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';
import { Container } from 'typedi';
import config from "../../../config";
import IElevatorController from "../../controllers/IControllers/IElevatorController";

const route = Router();

export default (app: Router) => {
  app.use('/elevators', route);

  const ctrl = Container.get(config.controllers.elevator.name) as IElevatorController;

  route.post('',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        coordX1: Joi.number().required(),
        coordY1: Joi.number().required(),
        coordX2: Joi.number().required(),
        coordY2: Joi.number().required(),
        floorId: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createElevator(req, res, next) );

    route.patch('',
    celebrate({
      body: Joi.object({
        id:Joi.string().required(),
        code: Joi.string(),
        coordX1: Joi.number(),
        coordY1: Joi.number(),
        coordX2: Joi.number(),
        coordY2: Joi.number(),
        floorId: Joi.string()
      })
    }),
    (req, res, next) => ctrl.updateElevator(req, res, next) );

  route.put('/:id',
  celebrate({
    body: Joi.object({
      id:Joi.string().required(),
      code: Joi.string().required(),
      coordX1: Joi.number().required(),
      coordY1: Joi.number().required(),
      coordX2: Joi.number().required(),
      coordY2: Joi.number().required(),
      floorId: Joi.string().required()
    }),
    params: {
      id: Joi.string().required() // Validação para o ID no URL
    }
  }),
  
  (req, res, next) => ctrl.updateElevator(req, res, next) );

    route.get('', 
    (req, res, next) => { ctrl.getAllElevators(req, res, next);
      
  });
    
}