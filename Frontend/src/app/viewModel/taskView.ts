export interface TaskViewModel {
    id: string;
    description: string;
    user: string;
    contact: number;
    type: 'Pickup' | 'Vigilance';
  }

  export interface Floor {
    description: string;
    floorId: string;
    floorNumber: number;
  }