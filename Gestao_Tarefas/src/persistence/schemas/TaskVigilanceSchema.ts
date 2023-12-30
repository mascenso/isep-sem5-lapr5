import { ITaskVigilancePersistence } from '../../dataschema/ITaskVigilancePersistence';
import mongoose from 'mongoose';

const TaskVigilanceSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    description: { type: String, unique: false },
    buildingId: { type: String, unique: false },
    floors: { type: [Object], unique: false },
    startPosition: { type: [Number], unique: false },
    endPosition: { type: [Number], unique: false },
    contactNumber: { type: Number, unique: false },
    user:{ type: Object, unique: false },
    approved:{type:Boolean, unique:false},
    pending:{type:Boolean, unique:false},
    planned: {type:Boolean, unique:false}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<ITaskVigilancePersistence & mongoose.Document>('TaskVigilance', TaskVigilanceSchema);
