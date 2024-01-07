import { Request, Response, NextFunction } from 'express';

export default interface ITaskController  {
  createTask(req: Request, res: Response, next: NextFunction);
  updateTask(req: Request, res: Response, next: NextFunction);
  createVigilanceTask(req: Request, res: Response, next: NextFunction);
  createPickupDeliveryTask(req: Request, res: Response, next: NextFunction);
  getAllTasks(req: Request, res: Response, next: NextFunction);
  getAllVigilancePendingTasks(eq: Request, res: Response, next: NextFunction);
  getAllPickupDeliveryPendingTasks(eq: Request, res: Response, next: NextFunction);
  getAllPendingTasks(req: Request, res: Response, next: NextFunction);
  getAllPickupDeliveryApprovedTasks(req: Request, res: Response, next: NextFunction);
  getAllVigilanceApprovedTasks(req: Request, res: Response, next: NextFunction);
 // getAllApprovedTasks(req: Request, res: Response, next: NextFunction);
  planearTarefas(req: Request, res: Response, next: NextFunction);
  getTasksByUserEmail(req: Request, res: Response, next: NextFunction);
  getTasksByStatus(req: Request, res: Response, next: NextFunction);
}
