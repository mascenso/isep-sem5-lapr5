import {Router} from "express";
import {Container} from "typedi";
import config from "../../../config";
import {celebrate, Joi} from "celebrate";
import IRoomController from "../../controllers/IControllers/IRoomController";
import {RoomType} from "../../domain/Room-agg/roomType";


const route = Router();

export default (app: Router) => {
  app.use('', route);

  const ctrl = Container.get(config.controllers.room.name) as IRoomController;

  route.post('/buildings/:buildingId/floors/:floorId/rooms',
    celebrate({
      body: Joi.object({
        name: Joi.string().required(),
        description: Joi.string(),
        roomType: Joi.string().valid(...Object.values(RoomType)).required()
      })
    }),
    (req, res, next) => ctrl.createRoom(req, res, next) );

  route.get('/buildings/:buildingId/floors/:floorId/rooms/:roomId',
    (req, res, next) => ctrl.getRoomById(req, res, next) );

};
