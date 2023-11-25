import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IRobotTypeController from "./IControllers/IRobotTypeController";
import IRobotTypeService from '../services/IServices/IRobotTypeService';
import IRobotTypeDTO from '../dto/IRobotTypeDTO';

import { Result } from "../core/logic/Result";
import TaskType from '../enums/taskType';

@Service()
export default class RobotTypeController implements IRobotTypeController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
      @Inject(config.services.robotType.name) private robotTypeServiceInstance : IRobotTypeService
  ) {}

  public async createRobotType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotTypeOrError = await this.robotTypeServiceInstance.createRobotType(req.body as IRobotTypeDTO) as Result<IRobotTypeDTO>;

      if (robotTypeOrError.isFailure) {
        return res.status(402).send();
      }

      const robotTypeDTO = robotTypeOrError.getValue();
      return res.json( robotTypeDTO ).status(201);
    }
    catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a robottype with this code.").send();
      }else{
        return next(e);
      }
    }
  };

  public async updateRobotType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotTypeOrError = await this.robotTypeServiceInstance.updateRobotType(req.body as IRobotTypeDTO) as Result<IRobotTypeDTO>;

      if (robotTypeOrError.isFailure) {
        return res.status(404).send();
      }

      const robotTypeDTO = robotTypeOrError.getValue();
      return res.status(201).json( robotTypeDTO );
    }
    catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a robottype with this code.").send();
      }else{
        return next(e);
      }
    }
  };

  public async findByDesignationOrTaskType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotTypeOrError = await this.robotTypeServiceInstance.findByDesignationOrTaskType(
                                req.query?.designation as string, req.query?.taskType as TaskType[]) as Result<IRobotTypeDTO[]>;
      if (robotTypeOrError.isFailure) {
        return res.status(404).json(`There are no Robots Type in the database with the provided designation or able to to execute that task type.`);
      }

      const robotTypeDTOList = robotTypeOrError.getValue();
      return res.json(robotTypeDTOList).status(200);

    }
    catch (e) {
      return next(e);
    }
  };

  public async getAllRobotTypes(req: Request, res: Response, next: NextFunction) {

    try {

      const robotTypeOrError = await this.robotTypeServiceInstance.getAllRobotTypes() as Result<IRobotTypeDTO[]>;

      if (robotTypeOrError.isFailure) {
        return res.status(402).json('There are no robots Types saved in the database.').send();
      }

      const robotDTO = robotTypeOrError.getValue();
      return res.json( robotDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

}
