export interface RobotTypeResponseDTO {
  id: string;
  designacao: string;
  tipoTarefas: string[],
}

export interface CreateRobotTypeRequestDTO {
  designacao: string;
  TipoTarefas: string[],
}
