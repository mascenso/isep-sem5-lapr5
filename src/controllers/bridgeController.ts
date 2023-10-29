import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import { Result } from "../core/logic/Result";
import IBridgeController from "./IControllers/IBridgeController";
import IBridgeService from "../services/IServices/IBridgeService";
import IBridgeDTO from "../dto/IBridgeDTO";
import config from "../../config";
import IBuildingService from '../services/IServices/IBuildingService';

@Service()
export default class BridgeController implements IBridgeController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.bridge.name) private bridgeServiceInstance: IBridgeService,
  ) { }

  public async createBridge(req: Request, res: Response, next: NextFunction) {

    try {
      const bridgeOrError = await this.bridgeServiceInstance.createBridge(req.body as IBridgeDTO) as Result<IBridgeDTO>;

      if (bridgeOrError.isFailure) {
        return res.status(402).json(bridgeOrError.error).send();
      }

      const bridgeDTO = bridgeOrError.getValue();
      return res.json(bridgeDTO).status(201);
    }
    catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a bridge with this code.").send();
      }else{
        return next(e);
      }
    }
  }

  public async updateBridge(req: Request, res: Response, next: NextFunction) {
    try {
      const bridgeOrError = await this.bridgeServiceInstance.updateBridge(req.body as IBridgeDTO, req.params.id) as Result<IBridgeDTO>;

      if (bridgeOrError.isFailure) {
        return res.status(404).send();
      }

      const robotDTO = bridgeOrError.getValue();
      return res.status(200).json( robotDTO );
    }
    catch (e) {
      if(e.code ==11000){
        return res.status(409).json("Already exist a bridge with this code.").send();
      }else{
        return next(e);
      }
    }
  };

  public async getAllBridges(req: Request, res: Response, next: NextFunction) {

    try {

      const bridgeOrError = await this.bridgeServiceInstance.getAllBridges() as Result<IBridgeDTO[]>;

      if (bridgeOrError.isFailure) {
        return res.status(402).json('Dont exist any bridge save on DB').send();
      }

      const bridgeDTO = bridgeOrError.getValue();
      return res.json(bridgeDTO).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getBridgesBetweenBuildings(req: Request, res: Response, next: NextFunction) {

    try {
      const building1 = req.query.building1;
      const building2 = req.query.building2;

      const bridgeOrError = await this.bridgeServiceInstance.getBridgesBetweenBuildings(building1 as string, building2 as string) as Result<IBridgeDTO[]>;

      if (bridgeOrError.isFailure) {
        return res.status(402).json('Dont exist any bridge save on DB').send();
      }

      const bridgeDTO = bridgeOrError.getValue();
      return res.json(bridgeDTO).status(201);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getBridgesForBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingId = req.params.id; // O ID do edifício do URL

        // Vai ao serviço buscar os pisos com passagem para outros edifícios.
        const result = await this.bridgeServiceInstance.getBridgesForBuilding(buildingId);

        if (result.isFailure) {
          return res.status(404).json(result.errorValue()).send();
        }

        const buildingBridges = result.getValue();

        return res.json(buildingBridges).status(200);
      } catch (e) {
        return next(e);
      }
    }
}
