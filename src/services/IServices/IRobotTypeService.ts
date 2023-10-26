import { Result } from "../../core/logic/Result";
import IRobotTypeDTO from "../../dto/IRobotTypeDTO";
import TaskType from "../../enums/taskType";

export default interface IRobotTypeService  {
  createRobotType(robotDTO: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>>;
  updateRobotType(robotDTO: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>>;
  findByDesignationOrTaskType(designation: string, taskType: TaskType[]): Promise<Result<IRobotTypeDTO[]>>;
}
