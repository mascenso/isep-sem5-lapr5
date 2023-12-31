import { Result } from "../../core/logic/Result";

export default interface IRouteService  {
  getRoutePlanning(routeInfo: any): Promise<Result<any>>;
}
