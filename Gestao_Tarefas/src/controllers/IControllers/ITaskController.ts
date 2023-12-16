import { Request, Response, NextFunction } from 'express';

export default interface ITaskController  {
  createTask(req: Request, res: Response, next: NextFunction);
  updateTask(req: Request, res: Response, next: NextFunction);

  getAllTasks(req: Request, res: Response, next: NextFunction);
}
