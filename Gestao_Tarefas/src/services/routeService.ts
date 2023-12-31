import { Service, Inject } from 'typedi';
import config from "../../config";
import ITaskDTO from '../dto/ITaskDTO';
import { Task } from "../domain/task";
import ITaskRepo from './IRepos/ITaskRepo';
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
import IRouteService from './IServices/IRouteService';


@Service()
export default class RouteService implements IRouteService {


  public async getRoutePlanning(routeInfo: any): Promise<Result<any>> {
    const axios = require('axios');
    //const url = `http://vs770.dei.isep.ipp.pt:8082/caminho?pisoOrigem=${piso1}&pisoDestino=${piso2}`;
    //const url = `http://rdg-planning.h5b0bhc4e5a0dddx.westeurope.azurecontainer.io/caminho?pisoOrigem=${piso1}&pisoDestino=${piso2}`;
    const url = `http://127.0.0.1:8081/caminho?pisoOrigem=${routeInfo.piso1}&pisoDestino=${routeInfo.piso2}`;

    try {
      const response = await axios.get(url); // Espera pela resposta da requisição
      return Result.ok(response.data);
    } catch (error) {
      console.error('Erro na chamada HTTP:', error);
      return Result.fail(error);
    }
  }

}
