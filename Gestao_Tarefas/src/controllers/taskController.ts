import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import ITaskController from "./IControllers/ITaskController";
import ITaskService from "../services/IServices/ITaskService";
import ITaskDTO from '../dto/ITaskDTO';

import { Result } from "../core/logic/Result";
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';

@Service()
export default class TaskController implements ITaskController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.task.name) private taskServiceInstance: ITaskService
  ) {
  }

  public async createTask(req: Request, res: Response, next: NextFunction) {
    try {
      const taskOrError = await this.taskServiceInstance.createTask(req.body as ITaskDTO) as Result<ITaskDTO>;

      if (taskOrError.isFailure) {
        return res.status(402).send();
      }

      const taskDTO = taskOrError.getValue();
      return res.json( taskDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async updateTask(req: Request, res: Response, next: NextFunction) {
    try {
      const taskOrError = await this.taskServiceInstance.updateTask(req.body as ITaskDTO) as Result<ITaskDTO>;

      if (taskOrError.isFailure) {
        return res.status(404).send();
      }

      const taskDTO = taskOrError.getValue();
      return res.status(201).json( taskDTO );
    }
    catch (e) {
      return next(e);
    }
  };

  public async getAllTasks(req: Request, res: Response, next: NextFunction) {
    try {
      const tasksOrError = await this.taskServiceInstance.getAllTasks() as Result<Array<ITaskDTO>>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json( tasksDTO );
    }
    catch (e) {
      return next(e);
    }
  }
}
