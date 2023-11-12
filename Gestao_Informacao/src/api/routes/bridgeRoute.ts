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
        floorAId: Joi.string().required(),
        floorBId: Joi.string().required(),
        })
    }),
    (req, res, next) => ctrl.createBridge(req, res, next) );

  route.put('/:id',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        name: Joi.string().required(),
        floorAId: Joi.string().required(),
        floorBId: Joi.string().required(),
      }),
    }),
    (req, res, next) => ctrl.updateBridge(req, res, next));

  route.get('',
    (req, res, next) => { ctrl.getAllBridges(req, res, next);
    });

  //  GET /api/bridges?building1={building1}&building2={building2}:
  route.get('/building',  (req, res, next) => { ctrl.getBridgesBetweenBuildings(req, res, next); }  );


  //Para listar pisos de um edifício com passagem para outros edifícios
  //  GET /api/bridges/building/:id
  route.get('/building/:id',
    (req, res, next) => { ctrl.getBridgesForBuilding(req, res, next);
  });

};