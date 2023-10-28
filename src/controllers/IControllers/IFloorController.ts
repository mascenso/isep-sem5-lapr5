import {NextFunction, Request, Response} from "express";

export default interface IFloorController {
  createFloor(req: Request, res: Response, next: NextFunction);
  addMapToFloor(req: Request, res: Response, next: NextFunction);
  updateFloor(req: Request, res: Response, next: NextFunction);
  getFloorsAtBuildings(req: Request, res: Response, next: NextFunction);
  getBuildingsByMinMaxFloors(req: Request, res: Response, next: NextFunction);
  getAllFloors(req: Request, res: Response, next: NextFunction);
}
