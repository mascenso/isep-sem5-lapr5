import { Service, Inject } from 'typedi';
import config from "../../config";
import ITaskDTO from '../dto/ITaskDTO';
import { Task } from "../domain/task";
import ITaskRepo from '../services/IRepos/ITaskRepo';
import ITaskService from './IServices/ITaskService';
import { Result } from "../core/logic/Result";
import { TaskMap } from "../mappers/TaskMap";
import ITaskVigilanceDTO from '../dto/ITaskVigilanceDTO'
import ITaskPickupDeliveryDTO from '../dto/ITaskPickupDeliveryDTO';
import { TaskVigilanceMap } from '../mappers/TaskVigilanceMap'
import ITaskVigilanceRepo from './IRepos/ITaskVigilanceRepo';
import ITaskPickupDeliveryRepo from './IRepos/ITaskPickupDeliveryRepo';
import { TaskPickupDeliveryMap } from '../mappers/TaskPickupDeliveryMap';
import { TaskVigilance } from '../domain/task-agg/TaskVigilance';
import { TaskPickupDelivery } from '../domain/task-agg/TaskPickupDelivery';
import axios, { AxiosResponse, AxiosError } from 'axios';



@Service()
export default class TaskService implements ITaskService {


  constructor(
    @Inject(config.repos.task.name) private taskRepo: ITaskRepo,
    @Inject(config.repos.taskVigilance.name) private taskVigilanceRepo: ITaskVigilanceRepo,
    @Inject(config.repos.taskPickupDelivery.name) private taskPickupDeliveryRepo: ITaskPickupDeliveryRepo,
  ) { }

  public async getTask(taskId: string): Promise<Result<ITaskDTO>> {
    try {
      const task = await this.taskRepo.findByDomainId(taskId);

      if (task === null) {
        return Result.fail<ITaskDTO>("Task not found");
      }
      else {
        const taskDTOResult = TaskMap.toDTO(task) as ITaskDTO;
        return Result.ok<ITaskDTO>(taskDTOResult)
      }
    } catch (e) {
      throw e;
    }
  }

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

  public async createTask(taskDTO: ITaskDTO): Promise<Result<ITaskDTO>> {
    try {

      const taskOrError = await Task.create(taskDTO);

      if (taskOrError.isFailure) {
        return Result.fail<ITaskDTO>(taskOrError.errorValue());
      }

      const taskResult = taskOrError.getValue();

      await this.taskRepo.save(taskResult);

      const taskDTOResult = TaskMap.toDTO(taskResult) as ITaskDTO;
      return Result.ok<ITaskDTO>(taskDTOResult)
    } catch (e) {
      throw e;
    }
  }

  public async getTasksPlanning(info: any): Promise<Result<any>> {
    const axios = require('axios');

    console.log("info ", info);

    //const formattedLTasks = encodeURIComponent(JSON.stringify(info.LTasks));
    //const apiUrl = `http://127.0.0.1:8081/tarefas?ltasks=${formattedLTasks}}&ng=${info.Ngeracoes}&dp=${info.dimensaoPop}&p1=${info.pobCruz}&p2=${info.pobMut}&t=${info.tempoLimite}&av=${info.avaliacaoDef}&nestab=${info.nEstabiliz}`;


    let url = `http://127.0.0.1:8081/tarefas?ltasks=[${info.LTasks}]&ng=${info.Ngeracoes}&dp=${info.dimensaoPop}&p1=${info.pobCruz}&p2=${info.pobMut}&t=${info.tempoLimite}&av=${info.avaliacaoDef}&nestab=${info.nEstabiliz}`;
    //let url = `http://vs770.dei.isep.ipp.pt:8082/tarefas?ng=${Ngeracoes}&dp=${dimensaoPop}&p1=${pobCruz}&p2=${pobMut}&t=${tempoLimite}&av=${avaliacaoDef}&nestab=${nEstabiliz}`;
    //let url = `http://rdg-planning.h5b0bhc4e5a0dddx.westeurope.azurecontainer.io/tarefas?ng=${Ngeracoes}&dp=${dimensaoPop}&p1=${pobCruz}&p2=${pobMut}&t=${tempoLimite}&av=${avaliacaoDef}&nestab=${nEstabiliz}`;
   // console.log("url ", url);
    try {
      const response = await axios.get(url); // Espera pela resposta da requisição
      console.log("dentro");
      return Result.ok(response.data);
    } catch (error) {
      console.error('Erro na chamada HTTP:', error);
      return Result.fail(error);
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

        const taskDTOResult = TaskMap.toDTO(task) as ITaskDTO;
        return Result.ok<ITaskDTO>(taskDTOResult)
      }
    } catch (e) {
      throw e;
    }
  }

  public async getAllTasks(): Promise<Result<Array<ITaskDTO>>> {
    try {
      const tasks = await this.taskRepo.findAll();

      const tasksDTO = tasks.map(task => TaskMap.toDTO(task) as ITaskDTO);

      return Result.ok<Array<ITaskDTO>>(tasksDTO)
    } catch (e) {
      throw e;
    }
  }


  public async getAllVigilancePendingTasks(): Promise<Result<Array<ITaskVigilanceDTO[]>>> {
    try {
      const allTasks = await this.taskVigilanceRepo.findAll();

      // Filtra apenas as tarefas onde 'pending' é true
      const filteredPendingTasks = allTasks.filter(task => task.pending === true);

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
      const filteredPendingTasks = allTasks.filter(task => task.pending === true);

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
      const filteredApprovedTasks = allTasks.filter(task => task.approved === true);
      const filteredApprovedNotPlannedTasks = filteredApprovedTasks.filter(task => task.planned === false);

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
      const filteredApprovedTasks = allTasks.filter(task => task.approved === true);
      const filteredApprovedNotPlannedTasks = filteredApprovedTasks.filter(task => task.planned === false);

      const pendingTasksDTO = filteredApprovedNotPlannedTasks.map((task) => TaskPickupDeliveryMap.toDTO(task));

      return Result.ok<Array<ITaskPickupDeliveryDTO[]>>([pendingTasksDTO]);
    } catch (error) {
      return Result.fail<Array<ITaskPickupDeliveryDTO[]>>(error); // Retorna falha em caso de erro
    }
  }

}
