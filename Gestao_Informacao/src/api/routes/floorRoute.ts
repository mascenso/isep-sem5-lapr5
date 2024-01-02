import { Router } from "express";
import Container from "typedi";
import config from "../../../config";
import { Joi, celebrate } from "celebrate";
import IFloorController from "../../controllers/IControllers/IFloorController";
import middlewares from "../middlewares";
import UserRole from "../../enums/userRole";

const route = Router();

export default (app: Router) => {
    app.use('/floors',
      middlewares.authRequest([
        UserRole.ADMINISTRATOR.toString(),
        UserRole.CAMPUS_MANAGER.toString(),
        UserRole.USER.toString()
      ]),
      route);

    const ctrl = Container.get(config.controllers.floor.name) as IFloorController;

    route.post('',
      celebrate({
        body: Joi.object({
            buildingId: Joi.string().required(),
            width: Joi.number().required(),
            length: Joi.number().required(),
            floorNumber: Joi.number().required(),
            description: Joi.string(),
            floorMap: Joi.object().optional()
        })
      }),
      (req, res, next) => ctrl.createFloor(req, res, next) );

    route.post('/:id/map',
    celebrate({
      params:Joi.object({
        id:Joi.string().required()
      }),
      body: Joi.object({
          floorMap: Joi.object().optional(),
      })
    }),
    (req, res, next) => {
      //junta o parametro id ao body para enviar no pedido
      req.body.id=req.params.id;
      ctrl.addMapToFloor(req, res, next) });

    route.patch('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        buildingId: Joi.string().required(),
        width: Joi.number(),
        length: Joi.number(),
        floorNumber: Joi.number(),
        description: Joi.string(),
        floorMap: Joi.object().optional(),
      })
    }),
    (req, res, next) => ctrl.updateFloor(req, res, next) );

  route.put('',
  celebrate({
    body: Joi.object({
      id: Joi.string().required(),
      buildingId: Joi.string().required(),
      width: Joi.number(),
      length: Joi.number(),
      floorNumber: Joi.number(),
      description: Joi.string(),
      floorMap: Joi.object().optional(),
    })
  }),
  (req, res, next) => ctrl.updateFloor(req, res, next) );

  //  GET /api/floors/buildings?building={building}
  route.get('/buildings',(req, res, next) => { ctrl.getFloorsAtBuildings(req, res, next); }  );

  route.get('',  (req, res, next) => { ctrl.getAllFloors(req, res, next); }  );

  //  GET /api/buildings?building1={building1}&building2={building2}:
  route.get('/buildings/minmaxfloors',  (req, res, next) => { ctrl.getBuildingsByMinMaxFloors(req, res, next); }  );

  route.get('/buildings/:buildingId/with-elevator',  (req, res, next) => { ctrl.getFloorsWithElevatorByBuildingId(req, res, next); }  );

  route.get('/:floorId',  (req, res, next) => { ctrl.getFloorById(req, res, next); }  );
};
