import { Service, Inject } from 'typedi';
import config from "../../config";
import ITaskDTO from '../dto/ITaskDTO';
import ITaskService from './IServices/ITaskService';
import { Result } from "../core/logic/Result";
import ITaskVigilanceDTO from '../dto/ITaskVigilanceDTO'
import ITaskPickupDeliveryDTO from '../dto/ITaskPickupDeliveryDTO';
import { TaskVigilanceMap } from '../mappers/TaskVigilanceMap'
import ITaskVigilanceRepo from './IRepos/ITaskVigilanceRepo';
import ITaskPickupDeliveryRepo from './IRepos/ITaskPickupDeliveryRepo';
import { TaskPickupDeliveryMap } from '../mappers/TaskPickupDeliveryMap';
import { TaskVigilance } from '../domain/task-agg/TaskVigilance';
import { TaskPickupDelivery } from '../domain/task-agg/TaskPickupDelivery';
import ITaskSearchResponseDTO from "../dto/ITaskSearchResponseDTO";
import { ParseUtils } from "../utils/ParseUtils";
import { TaskStatus } from "../domain/task-agg/TaskStatus";


@Service()
export default class TaskService implements ITaskService {

  private apiUrl = config.apiUrlPROLOG;

  constructor(
    @Inject(config.repos.taskVigilance.name) private taskVigilanceRepo: ITaskVigilanceRepo,
    @Inject(config.repos.taskPickupDelivery.name) private taskPickupDeliveryRepo: ITaskPickupDeliveryRepo,
  ) { }


  public async createVigilanceTask(taskVigilanceDTO: ITaskVigilanceDTO): Promise<Result<ITaskVigilanceDTO>> {
    try {

      const taskOrError = await TaskVigilance.create(taskVigilanceDTO);

      if (taskOrError.isFailure) {
        return Result.fail<ITaskVigilanceDTO>(taskOrError.errorValue());
      }

      const taskResult = taskOrError.getValue();

      await this.taskVigilanceRepo.save(taskResult);

      const taskDTOResult = TaskVigilanceMap.toDTO(taskResult) as ITaskVigilanceDTO;
      return Result.ok<ITaskVigilanceDTO>(taskDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async createPickupDeliveryTask(taskPickupDeliveryDTO: ITaskPickupDeliveryDTO): Promise<Result<ITaskPickupDeliveryDTO>> {
    try {

      const taskOrError = await TaskPickupDelivery.create(taskPickupDeliveryDTO);

      if (taskOrError.isFailure) {
        return Result.fail<ITaskPickupDeliveryDTO>(taskOrError.errorValue());
      }

      const taskResult = taskOrError.getValue();

      await this.taskPickupDeliveryRepo.save(taskResult);

      const taskDTOResult = TaskPickupDeliveryMap.toDTO(taskResult) as ITaskPickupDeliveryDTO;
      return Result.ok<ITaskPickupDeliveryDTO>(taskDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async getTasksPlanning(info: any): Promise<Result<any>> {
    const axios = require('axios');

    try {
      let urlPlanning = `${this.apiUrl}/tarefas?ng=${info.Ngeracoes}&dp=${info.dimensaoPop}&p1=${info.pobCruz}&p2=${info.pobMut}&t=${info.tempoLimite}&av=${info.avaliacaoDef}&nestab=${info.nEstabiliz}`;

      const response = await axios.get(urlPlanning); // Espera pela resposta da requisição

      return Result.ok(response.data);
    } catch (error) {
      console.error('Erro na chamada HTTP:', error);
      return Result.fail(error);
    }
  }


  public async updateTask(taskDTO: ITaskDTO, taskId:string): Promise<Result<ITaskPickupDeliveryDTO | ITaskVigilanceDTO>> {
    try {

      const statusEnum: TaskStatus = TaskStatus[taskDTO.taskStatus as keyof TaskStatus];

      taskDTO.id = taskId;
      const task_pickup = await this.taskPickupDeliveryRepo.findByDomainId(taskDTO.id);

      if (task_pickup === null) {
        const task_vig = await this.taskVigilanceRepo.findByDomainId(taskDTO.id);

        if (task_vig === null) {
          return Result.fail<ITaskVigilanceDTO>("Task not found");
        }
        else {

          task_vig.updateTaskStatus(statusEnum);
          await this.taskVigilanceRepo.save(task_vig);

          const taskDTOResult = TaskVigilanceMap.toDTO(task_vig) as ITaskVigilanceDTO;
          return Result.ok<ITaskVigilanceDTO>(taskDTOResult)
        }
      }
      else {
        task_pickup.updateTaskStatus(statusEnum);
        await this.taskPickupDeliveryRepo.save(task_pickup);

        const taskDTOResult = TaskPickupDeliveryMap.toDTO(task_pickup) as ITaskPickupDeliveryDTO;
        return Result.ok<ITaskPickupDeliveryDTO>(taskDTOResult)

      }
    } catch (e) {
      throw e;
    }
  }


  public async getAllVigilancePendingTasks(): Promise<Result<Array<ITaskVigilanceDTO[]>>> {
    try {
      const allTasks = await this.taskVigilanceRepo.findAll();

      // Filtra apenas as tarefas onde 'pending' é true
      const filteredPendingTasks = allTasks.filter(task => task.taskStatus.pending === true);

      const pendingTasksDTO = filteredPendingTasks.map((task) => TaskVigilanceMap.toDTO(task));

      return Result.ok<Array<ITaskVigilanceDTO[]>>([pendingTasksDTO]);
    } catch (error) {
      return Result.fail<Array<ITaskVigilanceDTO[]>>(error); // Retorna falha em caso de erro
    }
  }


  public async getAllPickupDeliveryPendingTasks(): Promise<Result<Array<ITaskPickupDeliveryDTO[]>>> {
    try {
      const allTasks = await this.taskPickupDeliveryRepo.findAll();

      // Filtra apenas as tarefas onde 'pending' é true
      const filteredPendingTasks = allTasks.filter(task => task.taskStatus.pending === true);

      const pendingTasksDTO = filteredPendingTasks.map((task) => TaskPickupDeliveryMap.toDTO(task));

      return Result.ok<Array<ITaskPickupDeliveryDTO[]>>([pendingTasksDTO]);
    } catch (error) {
      return Result.fail<Array<ITaskPickupDeliveryDTO[]>>(error); // Retorna falha em caso de erro
    }
  }

  public async getAllPendingTasks(): Promise<Result<Array<any[]>>> {
    try {
      const pickUptasksOrError = await this.getAllPickupDeliveryPendingTasks();
      const vigilanceTasksOrError = await this.getAllVigilancePendingTasks();

      if (pickUptasksOrError.isFailure || vigilanceTasksOrError.isFailure) {
        return Result.fail<Array<any[]>>([
          pickUptasksOrError.errorValue(),
          vigilanceTasksOrError.errorValue(),
        ]);
      }

      const allPendingTasks: any[] = [
        ...pickUptasksOrError.getValue(),
        ...vigilanceTasksOrError.getValue(),
      ];

      return Result.ok<Array<any[]>>(allPendingTasks);
    } catch (error) {
      return Result.fail<Array<any[]>>([error]); // Retorna falha em caso de erro
    }
  }

  public async getAllApprovedTasks(): Promise<Result<Array<any[]>>> {
    try {
      const pickUptasksOrError = await this.getAllPickupDeliveryApprovedTasks();
      const vigilanceTasksOrError = await this.getAllVigilanceApprovedTasks();

      if (pickUptasksOrError.isFailure || vigilanceTasksOrError.isFailure) {
        return Result.fail<Array<any[]>>([
          pickUptasksOrError.errorValue(),
          vigilanceTasksOrError.errorValue(),
        ]);
      }

      const allPendingTasks: any[] = [
        ...pickUptasksOrError.getValue(),
        ...vigilanceTasksOrError.getValue(),
      ];

      return Result.ok<Array<any[]>>(allPendingTasks);
    } catch (error) {
      return Result.fail<Array<any[]>>([error]); // Retorna falha em caso de erro
    }
  }


  public async getAllVigilanceApprovedTasks(): Promise<Result<Array<ITaskVigilanceDTO[]>>> {
    try {
      const allTasks = await this.taskVigilanceRepo.findAll();

      // Filtra apenas as tarefas onde 'approved' é true
      const filteredApprovedTasks = allTasks.filter(task => task.taskStatus.approved === true);
      const filteredApprovedNotPlannedTasks = filteredApprovedTasks.filter(task => task.taskStatus.planned === false);

      const pendingTasksDTO = filteredApprovedNotPlannedTasks.map((task) => TaskVigilanceMap.toDTO(task));

      return Result.ok<Array<ITaskVigilanceDTO[]>>([pendingTasksDTO]);
    } catch (error) {
      return Result.fail<Array<ITaskVigilanceDTO[]>>(error); // Retorna falha em caso de erro
    }
  }


  public async getAllPickupDeliveryApprovedTasks(): Promise<Result<Array<ITaskPickupDeliveryDTO[]>>> {
    try {
      const allTasks = await this.taskPickupDeliveryRepo.findAll();

      // Filtra apenas as tarefas onde 'pending' é true
      const filteredApprovedTasks = allTasks.filter(task => task.taskStatus.approved === true);
      const filteredApprovedNotPlannedTasks = filteredApprovedTasks.filter(task => task.taskStatus.planned === false);

      const pendingTasksDTO = filteredApprovedNotPlannedTasks.map((task) => TaskPickupDeliveryMap.toDTO(task));

      return Result.ok<Array<ITaskPickupDeliveryDTO[]>>([pendingTasksDTO]);
    } catch (error) {
      return Result.fail<Array<ITaskPickupDeliveryDTO[]>>(error); // Retorna falha em caso de erro
    }
  }

  public async getTasksByUserEmail(userEmail: string): Promise<Result<Array<ITaskSearchResponseDTO>>> {
    try {
      const deliveryTasks = await this.taskPickupDeliveryRepo.findByUserEmail(userEmail);
      const vigilanceTasks = await this.taskVigilanceRepo.findByUserEmail(userEmail);

      const tasksDTO = [];
      deliveryTasks.map(deliveryTask => tasksDTO.push(TaskPickupDeliveryMap.toSearchResponseDTO(deliveryTask)));
      vigilanceTasks.map(vigilanceTasks => tasksDTO.push(TaskVigilanceMap.toSearchResponseDTO(vigilanceTasks)));
      return Result.ok<Array<ITaskSearchResponseDTO>>(tasksDTO);

    }
    catch (error) {
      return Result.fail<Array<ITaskSearchResponseDTO>>(error);
    }

  }

  public async getTasksByStatus(taskStatus: string): Promise<Result<Array<ITaskSearchResponseDTO>>> {
    try {
      const status = ParseUtils.parseStringToTaskStatus(taskStatus);
      if (!status) {
        return Result.fail<Array<ITaskSearchResponseDTO>>('Invalid task status');
      }
      const deliveryTasks = await this.taskPickupDeliveryRepo.findByTaskStatus(status);
      const vigilanceTasks = await this.taskVigilanceRepo.findByTaskStatus(status);

      const tasksDTO = [];
      deliveryTasks.map(deliveryTask => tasksDTO.push(TaskPickupDeliveryMap.toSearchResponseDTO(deliveryTask)));
      vigilanceTasks.map(vigilanceTasks => tasksDTO.push(TaskVigilanceMap.toSearchResponseDTO(vigilanceTasks)));
      return Result.ok<Array<ITaskSearchResponseDTO>>(tasksDTO);

    }
    catch (error) {
      return Result.fail<Array<ITaskSearchResponseDTO>>(error);
    }

  }

}
