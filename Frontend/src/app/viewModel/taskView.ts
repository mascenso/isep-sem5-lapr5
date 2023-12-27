export class TaskViewModel {
    id: string;
    description: string;
    user: string; 
    contact: string; 

    constructor(taskData: any) {
        this.id = taskData.id;
        this.description = taskData.description;
        this.user = taskData.user.name; 
        this.contact = taskData.user.contacto; 
    }
}