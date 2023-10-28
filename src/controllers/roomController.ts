

import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from "../core/logic/Result";
import IRoomController from "./IControllers/IRoomController";
import IRoomService from "../services/IServices/IRoomService";
import {IRoomDTO} from "../dto/IRoomDTO";

@Service()
export default class RoomController implements IRoomController {
  constructor(
    @Inject(config.services.room.name) private roomServiceInstance : IRoomService
  ) {}

  public async createRoom(req: Request, res: Response, next: NextFunction) {

    try {
      const roomDto: IRoomDTO = {buildingId: req.params.buildingId, floorId: req.params.floorId, ...req.body};
      const roomOrError = await this.roomServiceInstance.createRoom(roomDto) as Result<IRoomDTO>;

      if (roomOrError.isFailure) {
        return res.status(409).send(roomOrError.errorValue());
      }

      const roomDTO = roomOrError.getValue();
      return res.json( roomDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getRoomById(req: Request, res: Response, next: NextFunction) {
    try {
      const roomOrError = await this.roomServiceInstance.getRoomById(req.params.roomId) as Result<IRoomDTO>;

      if (roomOrError.isFailure) {
        return res.status(404).send(roomOrError.errorValue());
      }

      const roomDTO = roomOrError.getValue();
      return res.json( roomDTO ).status(200);
    }
    catch (e) {
      return next(e);
    }
  }

}
