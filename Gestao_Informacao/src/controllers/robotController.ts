import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IRobotController from "./IControllers/IRobotController";
import IRobotService from '../services/IServices/IRobotService';
import IRobotDTO from '../dto/IRobotDTO';

import { Result } from "../core/logic/Result";

@Service()
export default class RobotController implements IRobotController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
      @Inject(config.services.robot.name) private robotServiceInstance : IRobotService
  ) {}

  public async createRobot(req: Request, res: Response, next: NextFunction) {
    try {
      const robotOrError = await this.robotServiceInstance.createRobot(req.body as IRobotDTO) as Result<IRobotDTO>;

      if (robotOrError.isFailure) {
        return res.status(402).json(robotOrError.error).send();
      }

      const robotDTO = robotOrError.getValue();
      return res.json( robotDTO ).status(201);
    }
    catch (e) {
        return next(e);
    }
  };

  public async updateRobot(req: Request, res: Response, next: NextFunction) {
    try {
      const robotOrError = await this.robotServiceInstance.updateRobot(req.body as IRobotDTO) as Result<IRobotDTO>;

      if (robotOrError.isFailure) {
        return res.status(404).send();
      }

      const robotDTO = robotOrError.getValue();
      return res.status(201).json( robotDTO );
    }
    catch (e) {
      return next(e);
    }
  };

  public async getAllRobots(req: Request, res: Response, next: NextFunction) {

    try {

      const robotOrError = await this.robotServiceInstance.getAllRobots(req.body as IRobotDTO) as Result<IRobotDTO[]>;

      if (robotOrError.isFailure) {
        return res.status(402).json('Dont exist any robots saves on DB').send();
      }

      const robotDTO = robotOrError.getValue();
      return res.json( robotDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async findByDesignationOrTaskType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotsOrError = await this.robotServiceInstance.findByDesignationOrTaskType(
                                req.query?.designation as string, req.query?.taskType as string) as Result<IRobotDTO[]>;
      if (robotsOrError.isFailure) {
        return res.status(404).json(`There are no Robots in the database with the provided designation or able to to execute that task type.`);
      }

      const robotDTOList = robotsOrError.getValue();
      return res.json(robotDTOList).status(200);

    }
    catch (e) {
      return next(e);
    }
  }
}
