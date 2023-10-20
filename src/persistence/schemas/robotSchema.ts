import { IRobotPersistence } from '../../dataschema/IRobotPersistence';
import mongoose from 'mongoose';

const RobotSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    designacao: { type: String, unique: true },
    tarefas: {type: String}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IRobotPersistence & mongoose.Document>('Robot', RobotSchema);
