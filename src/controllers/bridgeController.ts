import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';


import { Result } from "../core/logic/Result";
import IBridgeController from "./IControllers/IBridgeController";
import IBridgeService from "../services/IServices/IBridgeService";
import IBridgeDTO from "../dto/IBridgeDTO";
import config from "../../config";

@Service()
export default class BridgeController implements IBridgeController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.bridge.name) private bridgeServiceInstance : IBridgeService
  ) {}

  public async createBridge(req: Request, res: Response, next: NextFunction) {

    try {
      const bridgeOrError = await this.bridgeServiceInstance.createBridge(req.body as IBridgeDTO) as Result<IBridgeDTO>;

      if (bridgeOrError.isFailure) {
        return res.status(402).json(bridgeOrError.error).send();
      }

      const bridgeDTO = bridgeOrError.getValue();
      return res.json( bridgeDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  }
}
