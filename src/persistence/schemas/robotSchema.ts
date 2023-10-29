import { IRobotPersistence } from '../../dataschema/IRobotPersistence';
import mongoose from 'mongoose';

const RobotSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    nickName: { type: String, unique: true },
    robotType: {type: String, unique: false},
    serialNumber: {type: String, unique: true},
    description: {type: String, unique: false},
    inhibited: {type: Boolean, unique: false}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IRobotPersistence & mongoose.Document>('Robot', RobotSchema);
