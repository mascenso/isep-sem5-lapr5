import {NextFunction, Request, Response} from "express";

export default interface IRoomController  {
  createRoom(req: Request, res: Response, next: NextFunction);
  getRoomById(req: Request, res: Response, next: NextFunction);
}
