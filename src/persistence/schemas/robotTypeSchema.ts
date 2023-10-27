import { IRobotTypePersistence } from '../../dataschema/IRobotTypePersistence';
import mongoose from 'mongoose';
import TaskType from '../../enums/taskType';

const RobotTypeSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    designacao: { type: String, unique: true },
    tipoTarefas: [{ type: String, enum: Object.values(TaskType), unique: false }],

  },
  {
    timestamps: true
  }
);

export default mongoose.model<IRobotTypePersistence & mongoose.Document>('RobotType', RobotTypeSchema);
