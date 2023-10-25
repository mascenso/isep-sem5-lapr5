

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

  public async updateFloor(req: Request, res: Response, next: NextFunction) {

    try {

      const floorOrError = await this.floorServiceInstance.updateFloor(req.body as IFloorDTO) as Result<IFloorDTO>;
      
      if (floorOrError.isFailure) {
        return res.status(402).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json( floorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async addMapToFloor(req: Request, res: Response, next: NextFunction) {

    try {

      const floorOrError = await this.floorServiceInstance.addMapToFloor(req.body as IFloorDTO) as Result<IFloorDTO>;

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

  public async getFloorsAtBuildings(req: Request, res: Response, next: NextFunction) {

    try {
      const building = req.query.building;

      const floorOrError = await this.floorServiceInstance.getFloorsAtBuildings(building as string) as Result<IFloorDTO[]>;

      if (floorOrError.isFailure) {
        return res.status(402).json('Dont exist any floor saved on DB').send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json( floorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }
}