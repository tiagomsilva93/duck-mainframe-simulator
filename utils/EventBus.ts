
type Handler = (data?: any) => void;

class EventBus {
  private listeners: Map<string, Handler[]> = new Map();

  on(event: string, handler: Handler) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, []);
    }
    this.listeners.get(event)?.push(handler);
  }

  off(event: string, handler: Handler) {
    const handlers = this.listeners.get(event);
    if (handlers) {
      this.listeners.set(event, handlers.filter(h => h !== handler));
    }
  }

  emit(event: string, data?: any) {
    const handlers = this.listeners.get(event);
    if (handlers) {
      handlers.forEach(h => h(data));
    }
  }
}

export const SystemEvents = new EventBus();
