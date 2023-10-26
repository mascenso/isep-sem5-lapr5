import TaskType from "../enums/taskType";

export default interface IRobotTypeDTO {
  id: string;
  designacao: string;
  tipoTarefas: TaskType[];
}
