import { Request, Response, NextFunction } from 'express';

export default interface IRouteController  {
  planearRota(req: Request, res: Response, next: NextFunction);
}
