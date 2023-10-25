import {NextFunction, Request, Response} from "express";

export default interface IBridgeController {
  createBridge(req: Request, res: Response, next: NextFunction);
  getAllBridges(req: Request, res: Response, next: NextFunction);

  getBridgesAtBuildings(req: Request, res: Response, next: NextFunction);
}