import {NextFunction, Request, Response} from "express";

export default interface IFloorController {
  createFloor(req: Request, res: Response, next: NextFunction);
  //updateFloor(req: Request, res: Response, next: NextFunction);
}