

import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from "../core/logic/Result";
import IFloorController from "./IControllers/IFloorController";
import IFloorService from "../services/IServices/IFloorService";
import {IFloorDTO} from "../dto/IFloorDTO";

@Service()
export default class FloorController implements IFloorController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.floor.name) private floorServiceInstance : IFloorService
  ) {}

  public async createFloor(req: Request, res: Response, next: NextFunction) {

    try {
      const floorOrError = await this.floorServiceInstance.createFloor(req.body as IFloorDTO) as Result<IFloorDTO>;

      if (floorOrError.isFailure) {
        return res.status(402).json(floorOrError.error).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json( floorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }
}