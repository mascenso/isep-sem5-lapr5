import { Service, Inject } from 'typedi';
import config from "../../config";
import ITaskDTO from '../dto/ITaskDTO';
import { Task } from "../domain/task";
import ITaskRepo from '../services/IRepos/ITaskRepo';
import ITaskService from './IServices/ITaskService';
import { Result } from "../core/logic/Result";
import { TaskMap } from "../mappers/TaskMap";

@Service()
export default class TaskService implements ITaskService {
  constructor(
      @Inject(config.repos.task.name) private taskRepo : ITaskRepo
  ) {}

  public async getTask( taskId: string): Promise<Result<ITaskDTO>> {
    try {
      const task = await this.taskRepo.findByDomainId(taskId);

      if (task === null) {
        return Result.fail<ITaskDTO>("Task not found");
      }
      else {
        const taskDTOResult = TaskMap.toDTO( task ) as ITaskDTO;
        return Result.ok<ITaskDTO>( taskDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }


  public async createTask(taskDTO: ITaskDTO): Promise<Result<ITaskDTO>> {
    try {

      const taskOrError = await Task.create( taskDTO );

      if (taskOrError.isFailure) {
        return Result.fail<ITaskDTO>(taskOrError.errorValue());
      }

      const taskResult = taskOrError.getValue();

      await this.taskRepo.save(taskResult);

      const taskDTOResult = TaskMap.toDTO( taskResult ) as ITaskDTO;
      return Result.ok<ITaskDTO>( taskDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updateTask(taskDTO: ITaskDTO): Promise<Result<ITaskDTO>> {
    try {
      const task = await this.taskRepo.findByDomainId(taskDTO.id);

      if (task === null) {
        return Result.fail<ITaskDTO>("Task not found");
      }
      else {
        task.name = taskDTO.name;
        await this.taskRepo.save(task);

        const taskDTOResult = TaskMap.toDTO( task ) as ITaskDTO;
        return Result.ok<ITaskDTO>( taskDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async getAllTasks(): Promise<Result<Array<ITaskDTO>>> {
    try {
      const tasks = await this.taskRepo.findAll();

      const tasksDTO = tasks.map( task => TaskMap.toDTO( task ) as ITaskDTO );

      return Result.ok<Array<ITaskDTO>>( tasksDTO )
    } catch (e) {
      throw e;
    }
  }

}
