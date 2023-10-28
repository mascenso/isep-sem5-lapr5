

import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from "../core/logic/Result";
import IBuildingController from "./IControllers/IBuildingController";
import IBuildingService from "../services/IServices/IBuildingService";
import {IBuildingDTO} from "../dto/IBuildingDTO";

@Service()
export default class BuildingController implements IBuildingController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.building.name) private buildingServiceInstance : IBuildingService
  ) {}

  public async createBuilding(req: Request, res: Response, next: NextFunction) {

    try {
      const buildingOrError = await this.buildingServiceInstance.createBuilding(req.body as IBuildingDTO) as Result<IBuildingDTO>;

      if (buildingOrError.isFailure) {
        return res.status(402).send();
      }

      const buildingDTO = buildingOrError.getValue();
      return res.json( buildingDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async updateBuilding(req: Request, res: Response, next: NextFunction) {

    try {

      const buildingOrError = await this.buildingServiceInstance.updateBuilding(req.body as IBuildingDTO) as Result<IBuildingDTO>;
      
      if (buildingOrError.isFailure) {
        return res.status(402).send();
      }

      const buildingDTO = buildingOrError.getValue();
      return res.json( buildingDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getAllBuildings(req: Request, res: Response, next: NextFunction) {

    try {

      const buildingOrError = await this.buildingServiceInstance.getAllBuildings(req.body as IBuildingDTO) as Result<IBuildingDTO[]>;
      
      if (buildingOrError.isFailure) {
        return res.status(402).json('Dont exist any building save on DB').send();
      }

      const buildingDTO = buildingOrError.getValue();
      return res.json( buildingDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }


}
