import {NextFunction, Request, Response} from "express";

export default interface IBridgeController {
  createBridge(req: Request, res: Response, next: NextFunction);
  getAllBridges(req: Request, res: Response, next: NextFunction);
  getBridgesForBuilding(req: Request, res: Response, next: NextFunction);

  getBridgesBetweenBuildings(req: Request, res: Response, next: NextFunction);
  updateBridge(req: Request, res: Response, next: NextFunction);
}
