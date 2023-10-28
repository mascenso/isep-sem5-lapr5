import { Request, Response, NextFunction } from 'express';

export default interface IElevatorController  {
    createElevator(req: Request, res: Response, next: NextFunction);
    updateElevator(req: Request, res: Response, next: NextFunction);
    getAllElevators(req: Request, res: Response, next: NextFunction);
}