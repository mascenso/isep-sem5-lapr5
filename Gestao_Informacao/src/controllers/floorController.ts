

import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from "../core/logic/Result";
import IFloorController from "./IControllers/IFloorController";
import IFloorService from "../services/IServices/IFloorService";
import {IFloorDTO} from "../dto/IFloorDTO";
import { IBuildingDTO } from '../dto/IBuildingDTO';
import { BuildingId } from '../domain/building-agg/buildingId';

@Service()
export default class FloorController implements IFloorController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.floor.name) private floorServiceInstance: IFloorService
  ) {
  }

  public async createFloor(req: Request, res: Response, next: NextFunction) {

    try {
      const floorOrError = await this.floorServiceInstance.createFloor(req.body as IFloorDTO) as Result<IFloorDTO>;

      if (floorOrError.isFailure) {
        return res.status(402).json(floorOrError.error).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json(floorDTO).status(201);
    } catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a floor with this code.").send();
      }else{
        return next(e);
      }
    }
  }

  public async updateFloor(req: Request, res: Response, next: NextFunction) {

    try {

      const floorOrError = await this.floorServiceInstance.updateFloor(req.body as IFloorDTO) as Result<IFloorDTO>;

      if (floorOrError.isFailure) {
        return res.status(402).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json(floorDTO).status(201);
    } catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a floor with this code.").send();
      }else{
        return next(e);
      }
    }
  }

  public async addMapToFloor(req: Request, res: Response, next: NextFunction) {
    console.log(3333333)
    try {

      const floorOrError = await this.floorServiceInstance.addMapToFloor(req.body as IFloorDTO) as Result<IFloorDTO>;

      if (floorOrError.isFailure) {
        return res.status(402).json(floorOrError.error).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json(floorDTO).status(201);
    } catch (e) {
      return next(e);
    }
  }

  public async getFloorsAtBuildings(req: Request, res: Response, next: NextFunction) {

    try {
      const building = req.query.building;

      const floorOrError = await this.floorServiceInstance.getFloorsAtBuildings(building as string) as Result<IFloorDTO[]>;

      if (floorOrError.isFailure) {
        return res.status(402).json(`The building with id ${building} has no floors allocated.`).send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json(floorDTO).status(201);
    } catch (e) {
      return next(e);
    }
  }

  public async getAllFloors(req: Request, res: Response, next: NextFunction) {

    try {

      const floorOrError = await this.floorServiceInstance.getAllFloors() as Result<IFloorDTO[]>;

      if (floorOrError.isFailure) {
        return res.status(402).json('This building has no floors allocated.').send();
      }

      const floorDTO = floorOrError.getValue();
      return res.json(floorDTO).status(201);
    } catch (e) {
      return next(e);
    }
  }

  public async getBuildingsByMinMaxFloors(req: Request, res: Response, next: NextFunction) {
    try {
      const minFloors = parseInt(req.query.minFloors as string, 10);
      const maxFloors = parseInt(req.query.maxFloors as string, 10);

      const floorsOrError = await this.floorServiceInstance.getBuildingsByMinMaxFloors(minFloors, maxFloors) as Result<IBuildingDTO[]>;

      if (floorsOrError.isFailure) {
        return res.status(402).json({ errors: { message: 'No floors found in the given range' } }).send();
      }

      const floorDTOs = floorsOrError.getValue();
      return res.json(floorDTOs).status(200);
    } catch (e) {
      return next(e);
    }
  }

  public async getFloorsWithElevatorByBuildingId(req: Request, res: Response, next: NextFunction) {
    try {
      const floorsOrError = await this.floorServiceInstance.getFloorsWithElevatorByBuildingId(req.params.buildingId as string) as Result<IFloorDTO[]>;
      if (floorsOrError.isFailure) {
        return res.status(412).send(floorsOrError.error);
      }
      const floorDTOs = floorsOrError.getValue();

      return res.json(floorDTOs).status(200);

    } catch (e) {
      return next(e);
    }
  }

}
