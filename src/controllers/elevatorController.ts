import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from "../core/logic/Result";
import IElevatorController from "./IControllers/IElevatorController";
import IElevatorService from "../services/IServices/IElevatorService";
import {IElevatorDTO} from "../dto/IElevatorDTO";

@Service()
export default class ElevatorController implements IElevatorController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.elevator.name) private elevatorServiceInstance : IElevatorService
  ) {}

  public async createElevator(req: Request, res: Response, next: NextFunction) {

    try {
      const elevatorOrError = await this.elevatorServiceInstance.createElevator(req.body as IElevatorDTO) as Result<IElevatorDTO>;

      if (elevatorOrError.isFailure) {
        return res.status(402).json(elevatorOrError).send();
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json( elevatorDTO ).status(201);
      
    }
    catch (e) {
      return next(e);
    }
  }
/*
  public async updateElevator(req: Request, res: Response, next: NextFunction) {

    try {

      const elevatorOrError = await this.elevatorServiceInstance.updateElevator(req.body as IElevatorDTO) as Result<IElevatorDTO>;
      
      if (elevatorOrError.isFailure) {
        return res.status(402).send();
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json( elevatorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }
  */

  public async getAllElevators(req: Request, res: Response, next: NextFunction) {

    try {

      const elevatorOrError = await this.elevatorServiceInstance.getAllElevators(req.body as IElevatorDTO) as Result<IElevatorDTO[]>;
      
      if (elevatorOrError.isFailure) {
        return res.status(402).json('Dont exist any building save on DB').send();
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json( elevatorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

    
}
