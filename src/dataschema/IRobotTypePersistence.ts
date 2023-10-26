import TaskType from "../enums/taskType";

export interface IRobotTypePersistence {
  domainId: string;
  designacao: string;
  tipoTarefas: TaskType[];
}