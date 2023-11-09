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
      if(e.code ==11000){
        return res.status(409).json("Already exist a elevator with this code.").send();
      }else{
        return next(e);
      }
    }
  }

  public async updateElevator(req: Request, res: Response, next: NextFunction) {

    try {
 
      const elevatorOrError = await this.elevatorServiceInstance.updateElevator(req.body as IElevatorDTO) as Result<IElevatorDTO>;

      if (elevatorOrError.isFailure) {
        return res.status(402).json('Elevator does not exist.').send();
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json( elevatorDTO ).status(201);
    }
    catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a elevator with this code.").send();
      }else{
        return next(e);
      }
    }
  }

  public async getAllElevators(req: Request, res: Response, next: NextFunction) {

    try {

      const elevatorOrError = await this.elevatorServiceInstance.getAllElevators(req.body as IElevatorDTO) as Result<IElevatorDTO[]>;
      
      if (elevatorOrError.isFailure) {
        return res.status(402).json('There is no elevators in DB').send();
      }

      const elevatorDTO = elevatorOrError.getValue();
      return res.json( elevatorDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }    
}
