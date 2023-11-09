import {NextFunction, Request, Response} from "express";

export default interface IBuildingController {
  createBuilding(req: Request, res: Response, next: NextFunction);
  updateBuilding(req: Request, res: Response, next: NextFunction);
  getAllBuildings(req: Request, res: Response, next: NextFunction);
  //getBuildingsByMinMaxFloors(req: Request, res: Response, next: NextFunction);
  getBuildingById(req: Request, res: Response, next: NextFunction);
}
