import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import ITaskController from "./IControllers/ITaskController";
import ITaskService from "../services/IServices/ITaskService";
import ITaskDTO from '../dto/ITaskDTO';
import ITaskPickupDeliveryDTO from '../dto/ITaskPickupDeliveryDTO';
import ITaskVigilanceDTO from '../dto/ITaskVigilanceDTO';

import { Result } from "../core/logic/Result";


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
      return res.json(taskDTO).status(201);
    }
    catch (e) {
      return next(e);
    }
  };
  public async createVigilanceTask(req: Request, res: Response, next: NextFunction) {
    try {
      const taskOrError = await this.taskServiceInstance.createVigilanceTask(req.body as ITaskVigilanceDTO) as Result<ITaskVigilanceDTO>;

      if (taskOrError.isFailure) {
        return res.status(402).send();
      }

      const taskDTO = taskOrError.getValue();
      return res.json(taskDTO).status(201);
    }
    catch (e) {
      return next(e);
    }
  };
  public async createPickupDeliveryTask(req: Request, res: Response, next: NextFunction) {
    try {
      const taskOrError = await this.taskServiceInstance.createPickupDeliveryTask(req.body as ITaskPickupDeliveryDTO) as Result<ITaskPickupDeliveryDTO>;

      if (taskOrError.isFailure) {
        return res.status(402).send();
      }

      const taskDTO = taskOrError.getValue();
      return res.json(taskDTO).status(201);
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
      return res.status(201).json(taskDTO);
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
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getAllVigilancePendingTasks(req: Request, res: Response, next: NextFunction) {

    try {
      const tasksOrError = await this.taskServiceInstance.getAllVigilancePendingTasks() as Result<Array<ITaskVigilanceDTO[]>>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getAllPickupDeliveryPendingTasks(req: Request, res: Response, next: NextFunction) {

    try {
      const tasksOrError = await this.taskServiceInstance.getAllPickupDeliveryPendingTasks() as Result<Array<ITaskPickupDeliveryDTO[]>>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }
  }


  public async getAllPendingTasks(req: Request, res: Response, next: NextFunction) {


    try {
      const tasksOrError = await this.taskServiceInstance.getAllPendingTasks() as Result<Array<any[]>>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }

  }

  public async getAllApprovedTasks(req: Request, res: Response, next: NextFunction) {


    try {
      const tasksOrError = await this.taskServiceInstance.getAllApprovedTasks() as Result<Array<any[]>>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }

  }

  public async planearTarefas(req: Request, res: Response, next: NextFunction) {
    const taskInfo = req.body;
    try {

      const tasksOrError = await this.taskServiceInstance.getTasksPlanning(taskInfo) as Result<any>;

      if (tasksOrError.isFailure) {
        return res.status(404).send();
      }

      const tasksDTO = tasksOrError.getValue();
      return res.status(201).json(tasksDTO);
    }
    catch (e) {
      return next(e);
    }
  }
}

