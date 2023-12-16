import { ITaskPersistence } from '../../dataschema/ITaskPersistence';
import mongoose from 'mongoose';

const TaskSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    name: { type: String, unique: true }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<ITaskPersistence & mongoose.Document>('Task', TaskSchema);
