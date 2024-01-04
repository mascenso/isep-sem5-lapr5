import { Service} from 'typedi';
import { Result } from "../core/logic/Result";
import IRouteService from './IServices/IRouteService';
import config from '../../config';


@Service()
export default class RouteService implements IRouteService {

  private apiUrl = config.apiUrlPROLOG;

  public async getRoutePlanning(routeInfo: any): Promise<Result<any>> {
    const axios = require('axios');
    const url = `${this.apiUrl}/caminho?pisoOrigem=${routeInfo.piso1}&pisoDestino=${routeInfo.piso2}`;

    try {
      const response = await axios.get(url); // Espera pela resposta da requisição
      return Result.ok(response.data);
    } catch (error) {
      console.error('Erro na chamada HTTP:', error);
      return Result.fail(error);
    }
  }

}
