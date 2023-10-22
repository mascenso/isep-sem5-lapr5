import { Router } from "express";
import Container from "typedi";
import config from "../../../config";
import { Joi, celebrate } from "celebrate";
import IFloorController from "../../controllers/IControllers/IFloorController";

const route = Router();

export default (app: Router) => {
    app.use('/floors', route);
  
    const ctrl = Container.get(config.controllers.floor.name) as IFloorController;
  
    route.post('',
      celebrate({
        body: Joi.object({
            buildingId: Joi.string().required(),
            width: Joi.number().required(),
            length: Joi.number().required(),
            floorNumber: Joi.number().required(),
            description: Joi.string()
        })
      }),
      (req, res, next) => ctrl.createFloor(req, res, next) );

};