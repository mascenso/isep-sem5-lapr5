import {NextFunction, Request, Response} from "express";

export default interface IBridgeController {
  createBridge(req: Request, res: Response, next: NextFunction);
}
