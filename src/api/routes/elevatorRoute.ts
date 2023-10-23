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
        code: Joi.string(),
        coordX: Joi.number(),
        coordY: Joi.number()
      })
    }),
    (req, res, next) => ctrl.createElevator(req, res, next) );
/*
    route.patch('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        code: Joi.string(),
        coordX: Joi.number(),
        coordY: Joi.number()
      })
    }),
    (req, res, next) => ctrl.updateElevator(req, res, next) );

  route.put('',
  celebrate({
    body: Joi.object({
        id: Joi.string().required(),
        code: Joi.string(),
        coordX: Joi.number(),
        coordY: Joi.number()
    })

  }),
  (req, res, next) => ctrl.updateElevator(req, res, next) );
*/
    route.get('', 
    (req, res, next) => { ctrl.getAllElevators(req, res, next);
      
  });
    
}