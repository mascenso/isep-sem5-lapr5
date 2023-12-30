import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import ITaskController from "./IControllers/ITaskController";
import ITaskService from "../services/IServices/ITaskService";
import ITaskDTO from '../dto/ITaskDTO';
import ITaskPickupDeliveryDTO from '../dto/ITaskPickupDeliveryDTO';
import ITaskVigilanceDTO from '../dto/ITaskVigilanceDTO';
import IRouteController from './IControllers/IRouteController';
import IRouteService from '../services/IServices/IRouteService';

import { Result } from "../core/logic/Result";


@Service()
export default class RouteController implements IRouteController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.route.name) private routeServiceInstance: IRouteService
  ) {
  }

  public async planearRota(req: Request, res: Response, next: NextFunction) {
    const routeInfo = req.params;
    try {

      const planOrError = await this.routeServiceInstance.getRoutePlanning(routeInfo) as Result<any>;

      if (planOrError.isFailure) {
        return res.status(404).send();
      }

      const planDTO = planOrError.getValue();
      return res.status(201).json(planDTO);
    }
    catch (e) {
      return next(e);
    }
  }
  
}

