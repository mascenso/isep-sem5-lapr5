

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
      console.log(roomDto);
      const roomOrError = await this.roomServiceInstance.createRoom(roomDto) as Result<IRoomDTO>;

      if (roomOrError.isFailure) {
        return res.status(402).send();
      }

      const roomDTO = roomOrError.getValue();
      return res.json( roomDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

}
